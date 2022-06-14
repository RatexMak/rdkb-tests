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

package com.automatics.rdkb.tests.wifi;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

/**
 * Test class for Validating RDM Feature
 * 
 * @author Selvaraj Mariyappan,
 * 
 */


public class BroadBandWiFiRdmTest extends AutomaticsTestBase {
	
	/**
     * Test to verify rdm-manifest json in the device
     * <ol>
     * <li>Verify rdk-manifest.xml has been removed</li>
     * <li>Verify rdm-manifest.json is present in /etc/rdm directory</li>
     * <li>Verify rdm-manifest file is in json format</li>
     * </ol>
     * 
     * @author Ashwin sankara
     * @refactor yamini.s
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, enabled = true, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-RDM-1006")
    public void testVerifyRdmManifestJson(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-RDM-006";
	String stepNum = "s1";
	String errorMessage = null;
	String response = null;
	boolean status = false;
	// Variable Declation Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-RDM-1006");
	LOGGER.info("TEST DESCRIPTION: Test to verify rdm-manifest json in the device");

	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify rdk-manifest.xml has been removed");
	LOGGER.info("2. Verify rdm-manifest.json is present in /etc/rdm directory");
	LOGGER.info("3. Verify rdm-manifest file is in json format");

	LOGGER.info("#######################################################################################");

	try {

	    errorMessage = "File rdk-manifest.xml is not removed from the device";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify rdk-manifest.xml has been removed");
	    LOGGER.info("STEP 1: ACTION : Execute command:find / -iname \"rdk-manifest.xml\"");
	    LOGGER.info("STEP 1: EXPECTED : File is not present in the device");
	    LOGGER.info("**********************************************************************************");

	    status = !CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_PATH_RDK_MANIFEST_XML);

	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : File is not present in the device");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s2";
	    errorMessage = "File rdm-manifest.json is not present in /etc/rdm directory";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify rdm-manifest.json is present in /etc/rdm directory");
	    LOGGER.info("STEP 2: ACTION : Execute command:ls /etc/rdm/rdm-manifest.json");
	    LOGGER.info("STEP 2: EXPECTED : File is present in the device");
	    LOGGER.info("**********************************************************************************");

	    status = CommonUtils.isFileExists(device, tapEnv, BroadBandCommandConstants.FILE_PATH_RDM_MANIFEST_JSON);

	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : File is present in the device");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, true);

	    LOGGER.info("**********************************************************************************");

	    stepNum = "s3";
	    errorMessage = "File /etc/rdm/rdm-manifest.json contents are empty";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify rdm-manifest file is in json format");
	    LOGGER.info("STEP 3: ACTION : Execute command:cat /etc/rdm/rdm-manifest.json");
	    LOGGER.info("STEP 3: EXPECTED : File is present in json format");
	    LOGGER.info("**********************************************************************************");

	    response = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandTestConstants.CAT_COMMAND, BroadBandCommandConstants.FILE_PATH_RDM_MANIFEST_JSON));
	    if (CommonMethods.isNotNull(response)) {
		errorMessage = "File rdm-manifest.json is not present in json format";
		status = CommonMethods.isValidJsonString(response);
	    }

	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : File is present in json format");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }

	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    LOGGER.info("**********************************************************************************");

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, false, errorMessage,
		    false);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-RDM-1006");
    }

}
