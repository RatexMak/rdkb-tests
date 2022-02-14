/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.docsis;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadbandPropertyFileHandler;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandDocsisTest extends AutomaticsTestBase {
    /**
     * Test to validate the timestamp for upstream/downstream ranging
     * 
     * <li>1. Perform factory reset on the unit via WebPa</li>
     * <li>2. Validate if /rdklogs/logs/CMlog.txt.0 file is present</li>
     * <li>3. Get the expected response for downstream and upstream from CMlog.txt.0</li>
     * 
     * @param device
     * 
     * @author Sathurya_R
     * @refactor Govardhan
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-DOWNUPSTREAM-1001")
    public void testVerifyTimestampUpstreamDownstream(Dut device) {

	// Variable declaration begins
	String testCaseId = "";
	String stepNum = "S1";
	String errorMessage = "";
	boolean status = false;
	String response = null;
	// Variable declaration ends

	testCaseId = "TC-RDKB-DOWNUPSTREAM-001";

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-DOWNUPSTREAM-1001");
	LOGGER.info("TEST DESCRIPTION: Validate the timestamp for upstream/downstream ranging ");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Perform factory reset on the unit via WebPa ");
	LOGGER.info("2. Validate if /rdklogs/logs/CMlog.txt.0 file is present ");
	LOGGER.info("3. Get the expected response for downstream and upstream from CMlog.txt.0 ");

	LOGGER.info("#######################################################################################");

	try {

	    errorMessage = "Attempt to perform factory reset via webpa has failed";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1: DESCRIPTION : Perform factory reset on the unit via WebPa ");
	    LOGGER.info("STEP 1: ACTION : a) Execute WebPa SET command on the object "
		    + "Device.X_CISCO_COM_DeviceControl.FactoryReset using the command, "
		    + "\"curl --request GET --url '"+BroadbandPropertyFileHandler.getWebpaServerURL()+"<ECM MAC>/config?" + "names=Device.X_CISCO_COM_DeviceControl.FactoryReset' "
		    + "--header 'authorization: Bearer <SAT-TOKEN>'\"");
	    LOGGER.info(
		    "STEP 1: EXPECTED : The parameter should get set successfully and return a 200 success response");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandCommonUtils.performFactoryResetWebPa(tapEnv, device);

	    if (status) {
		LOGGER.info("STEP 1 ACTUAL : Attempt to factory reset via WEBPA is successfull!");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    errorMessage = "The log file /rdklogs/logs/CMlog.txt.0 is not present even after 10 minutes of waitime";
	    status = false;

	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 2: DESCRIPTION : Validate if /rdklogs/logs/CMlog.txt.0 file is present ");
	    LOGGER.info("STEP 2: ACTION : execute the following command inside the RG console of the gateway, "
		    + "\"if [ -f /rdklogs/logs/CMLog.txt.0 ] ; then echo \"true\" ; else echo \"false\" ; fi\"");
	    LOGGER.info("STEP 2: EXPECTED : The file /rdklogs/logs/CMLog.txt.0 parameter should present ");
	    LOGGER.info("#####################################################################################");

	    status = BroadBandCommonUtils.doesFileExistWithinGivenTimeFrameInArm(tapEnv, device,
		    BroadBandTestConstants.CM_LOG_FILE_NAME, BroadBandTestConstants.TEN_MINUTE_IN_MILLIS,
		    BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : The log file /rdklogs/logs/CMLog.txt.0 is present on the device");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    stepNum = "S3";
	    errorMessage = "The expected logs are not coming up in /rdklogs/logs/CMLog.txt.0 ";
	    status = false;
	    long startTimeStamp = System.currentTimeMillis();

	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Get the expected response for downstream and upstream from CMlog.txt.0 ");
	    LOGGER.info("STEP 3: ACTION : execute the following command inside the RG console of the gateway, "
		    + "\"cat /rdklogs/logs/CMlog.txt.0 |grep -E \"Downstream|Upstream\"");
	    LOGGER.info("STEP 3: EXPECTED : The expected logs should be present ");
	    LOGGER.info("#####################################################################################");

	    do {

		response = tapEnv.executeCommandUsingSsh(device,
			BroadBandCommandConstants.CMD_TO_GET_UPSTREAM_DOWNSTREAM_LOG);
		LOGGER.info("Response is : "+response);
		status = CommonMethods.isNotNull(response)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCH_DOWNSTREAM_LOCK)
			&& CommonMethods.patternMatcher(response, BroadBandTestConstants.PATTERN_MATCH_UPSTREAM_LOCK);

	    } while (!status
		    && (System.currentTimeMillis() - startTimeStamp) < BroadBandTestConstants.FIVE_MINUTE_IN_MILLIS
		    && CommonMethods.hasWaitForDuration(tapEnv, BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS));

	    if (status) {
		LOGGER.info(
			"STEP 3: ACTUAL : The expected downstream/upstream logs are present in /rdklogs/logs/CMlog.txt.0");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	} catch (Exception exeception) {
	    errorMessage = "Exception occurred while validating the the timestamp for upstream/downstream ranging"
		    + exeception.getMessage();
	    LOGGER.error(errorMessage);
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    /**
	     * POST-CONDITION 1 : ENABLE THE XIFNITY WIFI
	     */
	    BroadBandPostConditionUtils.executePostConditionToEnableXfinityWifi(device, tapEnv,
		    BroadBandTestConstants.CONSTANT_1);

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}

    }
}
