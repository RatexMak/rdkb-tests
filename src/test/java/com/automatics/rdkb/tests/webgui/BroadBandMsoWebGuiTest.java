/**
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

package com.automatics.rdkb.tests.webgui;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.webpa.WebPaServerResponse;

/**
 * Test class with test case for validating web ui based tests
 * 
 * @Refactor Alan_Bivera
 */
public class BroadBandMsoWebGuiTest extends BroadBandWebUiBaseTest {

    /**
     * Test to Validate the default value of band steering Capacity using TR181 data object via WEBPA and verify this
     * Parameter is read only.
     * 
     * <ol>
     * <li>STEP 1:Get the default value of Band Steering capability</li>
     * <li>STEP 2:Set the Band Steering capability as false</li>
     * </ol>
     * 
     * @param device
     * @author anandam.s
     * @Refactor Alan_Bivera
     */
    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-BAND-STEERING-1001", testDecription = "Test to Validate the default value of band steering Capacity using TR181 data object via WEBPA and verify this paraeter is read only")
    public void testVerifyBandSteeringCapability(Dut device) {
	// Test case id
	String testId = "TC-RDKB-WIFI-BAND-STEERING-101";
	// Test step number
	String testStepNumber = "s1";
	// result variable
	boolean status = false;
	// String to store the error message
	String errorMessage = null;
	// bandsteering status
	boolean defaultBandSteering = true;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: " + testId);
	LOGGER.info(
		"TEST DESCRIPTION: Validate the default value of band steering Capacity using TR181 data object via WEBPA and verify this paraeter is read only");
	LOGGER.info("*************************************************************************");

	LOGGER.info("************************************************************************************************");
	LOGGER.info("STEP 1:Get the default value of Band Steering capability  ");
	LOGGER.info("EXPECTED: Capability must always report  true  as value. ");
	LOGGER.info("STEP 2:Set the  Band Steering capability  as false");
	LOGGER.info(
		"EXPECTED: Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is a read only webpa and so the reponse should have \"Parameter is not writable\"");

	LOGGER.info("******************************************************");
	LOGGER.info("STEP 1: DESCRIPTION : Get the default value of Band Steering capability");
	LOGGER.info("STEP 1: ACTION      : Execute webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability ");
	LOGGER.info("STEP 1: EXPECTED    : Capability must always report  true  as value.  ");
	LOGGER.info("******************************************************");
	testStepNumber = "s1";
	String bandSteeringCapabilityValue = null;
	errorMessage = "Failed to get the default value of webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability";
	try {
	    bandSteeringCapabilityValue = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY);
	    LOGGER.info("Webpa response for bandSteeringCapabilityValue  :  " + bandSteeringCapabilityValue);
	    status = Boolean.valueOf(bandSteeringCapabilityValue);
	} catch (TestException e) {
	    errorMessage = e.getMessage();
	    LOGGER.error(errorMessage);
	}

	LOGGER.info("STEP 1: ACTUAL: "
		+ (status
			? "Default value of webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is "
				+ defaultBandSteering
			: errorMessage));
	tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	LOGGER.info("******************************************************");
	LOGGER.info("STEP 2: DESCRIPTION : Set the  Band Steering capability  as false");
	LOGGER.info("STEP 2: ACTION      : set webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability  as false ");
	LOGGER.info(
		"STEP 2: EXPECTED    :Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is a read only webpa and so the reponse should have \"Parameter is not writable\" ");
	LOGGER.info("******************************************************");
	testStepNumber = "s2";
	errorMessage = "Failed to verify  webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is read only ";
	LOGGER.info("Verifying  webpa Device.WiFi.X_RDKCENTRAL-COM_BandSteering.Capability is read only");
	try {
	    WebPaServerResponse serverResponse = BroadBandWebPaUtils.setWebPaParamAndReturnResp(tapEnv, device,
		    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.CONSTANT_3);
	    BroadBandResultObject result = BroadBandWebPaUtils
		    .verifyReadOnlyAtributeOfWebPaParamFromWebPaServerResponse(serverResponse,
			    BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    LOGGER.info("STEP 2: ACTUAL: "
		    + (status
			    ? BroadBandWebPaConstants.WEBPA_PARAM_BAND_STEERING_CAPABILITY
				    + " cannot be writable using WebPA, READ-ONLY attribute verified"
			    : errorMessage));
	} catch (TestException e) {
	    LOGGER.error(e.getMessage());
	}
	tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	LOGGER.debug("ENDING TESTCASE :testVerifyBandSteeringCapability() ");
    }
}
