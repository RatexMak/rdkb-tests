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
package com.automatics.rdkb.tests.webpa;

import java.util.ArrayList;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.cdl.FirmwareDownloadUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandWebPaTests extends AutomaticsTestBase {
    /**
     * 
     * This method verifies that webpa request to get value of Webpa.version parameter gives the value of WebPA version
     * and and configparamgen version and it's compatibility with Firmware version
     * 
     * <ol>
     * <li>Step 1 : Verify retrieval of WebPA version in TR181 parameter</li>
     * <li>Step 2 :Verify the configparamgen version running in gateway</li>
     * </ol>
     * 
     * @param device
     *            Dut to be used for execution
     * 
     * @author Ashwin Sankarasubramanian, Ashwin Sankara
     * @refactor Govardhan
     * 
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.NEW_FEATURE, BroadBandTestGroup.WEBPA })
    @TestDetails(testUID = "TC-RDKB-WEBPA-1003")
    public void testVerifyWebPAVersion(Dut device) {
	// Variables declaration starts
	boolean status = false;
	String testId = "TC-RDKB-WEBPA-003";
	String errorMessage = null;
	String response = null;
	String stepNum = null;
	String webpaVersion = null;
	ArrayList<String> patternMatchedStringList = new ArrayList<>();
	String configparamgenVersion = null;
	String currentBuild = null;
	// Variables declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WEBPA-1003");
	LOGGER.info(
		"TEST DESCRIPTION: Verify the retrieval of webpa version from tr181 parameter and configparamgen version and it's compatibility with Firmware version");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify WebPA version obtained using WebPA request.");
	LOGGER.info("2. Verify the configparamgen version running in gateway");
	LOGGER.info("#######################################################################################");
	try {
	    stepNum = "S1";
	    errorMessage = "Unable to get the webpa version.";
	    LOGGER.info("*****************************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify WebPA version obtained using WebPA request.");
	    LOGGER.info("STEP 1: ACTION : ACTION: Execute the TR-181 parameter-Device.X_RDKCENTRAL-COM_Webpa.Version.");
	    LOGGER.info("STEP 1: EXPECTED : WebPA request response contains WebPA version.");
	    LOGGER.info("*****************************************************************************************");
	    response = tapEnv.executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_WEBPA_VERSION);
	    if (CommonMethods.isNotNull(response)
		    && response.contains(BroadBandTestConstants.PROCESS_NAME_WEBPA.toUpperCase())) {
		patternMatchedStringList = CommonMethods.patternFinderToReturnAllMatchedString(response,
			BroadBandTestConstants.WEBPA_VERSION_PATTERN_MATCHER);
		if (patternMatchedStringList.size() == BroadBandTestConstants.CONSTANT_2) {
		    webpaVersion = patternMatchedStringList.get(BroadBandTestConstants.CONSTANT_0) + "."
			    + patternMatchedStringList.get(BroadBandTestConstants.CONSTANT_1);
		}
	    }
	    status = CommonMethods.isNotNull(webpaVersion);
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : WebPA request response contains WebPA version: " + webpaVersion);
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

	    stepNum = "S2";
	    status = false;
	    errorMessage = "Failed to get the configparamgen or current build details from the Gateway";
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2 : DESCRIPTION : Verify the configparamgen version running in gateway");
	    LOGGER.info("STEP 2 : ACTION : Execute command: configparamgen in gateway");
	    LOGGER.info(
		    "STEP 2 : EXPECTED : Must return the configparamgen version based on build varient mentioned below :\n"
			    + " 1. Release < 4.4                      : configparamgen Version : 2.17 \n"
			    + "	2. Release > 4.4                      : configparamgen Version : 3.7 or higher \n"
			    + "	3. 4.4 initial releases until 4.4p1s9 : configparamgen Version : 3.7 or higher \n"
			    + "	4. Release 4.4p1s10 to 4.4p2s2        : configparamgen Version : 2.17 \n"
			    + "	5. Release >=4.4p3s1                  : configparamgen Version : 3.7 or higher \n"
			    + "	6. Stable2                            : configparamgen Version : 3.7 or higher \n"
			    + "	7. Sprint                             : configparamgen Version : 3.7 or higher");
	    LOGGER.info("**********************************************************************************");
	    try {
		currentBuild = FirmwareDownloadUtils.getCurrentFirmwareFileNameForCdl(tapEnv, device);
		response = tapEnv.executeCommandUsingSsh(device, BroadBandCommandConstants.CONFIGPARAMGEN);
		LOGGER.info("Current Build in Gateway is : " + currentBuild);
		LOGGER.info("Response is : " + response);
		if (CommonUtils.isNotEmptyOrNull(response) && CommonUtils.isNotEmptyOrNull(currentBuild)) {
		    configparamgenVersion = CommonMethods.patternFinder(response,
			    BroadBandTestConstants.CONFIGPARAMGEN_VERSION_PATTERN_MATCHER);
		    LOGGER.info("Configparamgen Version obtained is : " + configparamgenVersion);
		    if (CommonUtils.isNotEmptyOrNull(configparamgenVersion)) {
			errorMessage = "configparemgen Required for build is not as expected";
			status = BroadBandCommonUtils.verifyConfigparamgenForGivenBuild(currentBuild,
				configparamgenVersion);
		    }
		}
	    } catch (Exception e) {
		LOGGER.error(errorMessage += e.getMessage());
	    }
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : Configparamgen Version is as Expected : " + configparamgenVersion);
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testId, stepNum, status, errorMessage, true);

	} catch (Exception exception) {
	    LOGGER.info("Inside catch");
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured while validating webpa version using webpa request: " + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testId, stepNum, status, errorMessage, true);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WEBPA-1003");
    }

}
