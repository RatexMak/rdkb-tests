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
package com.automatics.rdkb.tests.speedtest;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.dmcli.DmcliUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;

public class BroadBandSpeedTest extends AutomaticsTestBase {
	
	/**
     * Test to verify the speed test feature of Broadband device for ipv6 interface
     * 
     * all the commands are to be executed in atom console if present else in arm console.
     * 
     * <ol>
     * <li>Verify the Speed test Authentication TR-181 param can be set to a value with 2000 characters which is
     * approximately equal to 2048 bytes using DMCLI</li>
     * <li>Verify the Speed test Authentication TR-181 param can be retrieved using DMCLI after setting with 2000
     * character</li>
     * <li>Verify the Speed test Authentication TR-181 param can be set to a value with 2000 characters which is
     * approximately equal to 2048 bytes using WEBPA</li>
     * <li>Verify the Speed test Authentication TR-181 param can be retrieved using WEBPA after setting with 2000
     * character</li>
     * </ol>
     * 
     * @refactor yamini.s
     */
    @Test(dataProvider = DataProviderConstants.PARALLEL_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true)
    @TestDetails(testUID = "TC-RDKB-ST-AUT-5001")
    public void testToVerifySpeedTestAuthenticationTr181ObjectSize(Dut device) {

	// Variable Declaration begins
	String testCaseId = "TC-RDKB-ST-AUT-501";
	String stepNum = "";
	String errorMessage = "";
	boolean status = false;
	// Variable Declaration Ends

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-ST-AUT-5001");
	LOGGER.info("TEST DESCRIPTION: Verify the Speed test Authentication TR-181 object size is increased to 2048 bytes from 512 bytes");
	
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("1. Verify the Speed test Authentication TR-181 param can be set to a value with 2000 characters which is approximately equal to 2048 bytes using DMCLI");
	LOGGER.info("2. Verify the Speed test Authentication TR-181 param can be retrieved using DMCLI after setting with 2000 character");
	LOGGER.info("3. Verify the Speed test Authentication TR-181 param can be set to a value with 2000 characters which is approximately equal to 2048 bytes using WEBPA");
	LOGGER.info("4. Verify the Speed test Authentication TR-181 param can be retrieved using WEBPA after setting with 2000 character");
	LOGGER.info("#######################################################################################");

	try {

	    stepNum = "s1";
	    errorMessage = "Unable to set Speed test Authentication with 2000 characters using DMCLI";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Verify the Speed test Authentication TR-181 param can be set to a value with 2000 characters which is approximately equal to 2048 bytes using DMCLI");
	    LOGGER.info("STEP 1: ACTION : Execute the command : dmcli eRT setv Device.IP.Diagnostics.X_RDKCENTRAL-COM_SpeedTest.Authentication string \"<SAMPLE 2000 CHARACTER STRING>\"");
	    LOGGER.info("STEP 1: EXPECTED : DMCLI set operation should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = DmcliUtils.setParameterValueUsingDmcliCommand(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_SPEED_TEST_AUTHENTICATION,
		    BroadBandTestConstants.DMCLI_SUFFIX_TO_SET_STRING_PARAMETER, BroadBandCommonUtils
			    .concatStringUsingStringBuffer(BroadBandTestConstants.TEXT_DOUBLE_QUOTE,
				    BroadBandTestConstants.SAMPLE_STRING_WITH_2000_CHARACTER,
				    BroadBandTestConstants.TEXT_DOUBLE_QUOTE));
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL : Sample string with 2000 characters, approximately equal to 2048 bytes is successfully set to Speed test Authentication TR-181 param using DMCLI");
	    } else {
		LOGGER.error("STEP 1: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s2";
	    errorMessage = "Unable to retrieve Speed test Authentication using DMCLI";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Verify the Speed test Authentication TR-181 param can be retrieved using DMCLI after setting with 2000 character");
	    LOGGER.info("STEP 2: ACTION : Execute the command : dmcli eRT getv Device.IP.Diagnostics.X_RDKCENTRAL-COM_SpeedTest.Authentication");
	    LOGGER.info("STEP 2: EXPECTED : The returned value should be a 2000 character length");
	    LOGGER.info("**********************************************************************************");
	    String response = DmcliUtils.getParameterValueUsingDmcliCommand(device,
		    tapEnv, BroadBandWebPaConstants.WEBPA_PARAM_SPEED_TEST_AUTHENTICATION);
	    errorMessage = "Null/invalid response retrieved from DMCLI for Speed test Authentication";
	    status = CommonMethods.isNotNull(response)
		    && BroadBandTestConstants.SAMPLE_STRING_WITH_2000_CHARACTER.equalsIgnoreCase(response);
	    if (status) {
		LOGGER.info("STEP 2: ACTUAL : The returned value is a 2000 character length string");
	    } else {
		LOGGER.error("STEP 2: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s3";
	    errorMessage = "Unable to set Speed test Authentication with 2000 characters using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 3: DESCRIPTION : Verify the Speed test Authentication TR-181 param can be set to a value with 2000 characters which is approximately equal to 2048 bytes using WEBPA");
	    LOGGER.info("STEP 3: ACTION : Execute the command : curl -4 -k -H \"Authorization: Bearer <SAT_TOKEN>\"  -X PATCH <webpa url>device/mac:<CM_MAC>/config -d \"{\"parameters\":[{\"dataType\":0,\"name\":\"Device.IP.Diagnostics.X_RDKCENTRAL-COM_SpeedTest.Authentication\",\"value\":\"<SAMPLE 2000 CHARACTER STRING>\"}]}\"");
	    LOGGER.info("STEP 3: EXPECTED : WebPA set operation should be successful");
	    LOGGER.info("**********************************************************************************");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_SPEED_TEST_AUTHENTICATION, BroadBandTestConstants.CONSTANT_0,
		    BroadBandTestConstants.SAMPLE_STRING_WITH_2000_CHARACTER);
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL : Sample string with 2000 characters, approximately equal to 2048 bytes is successfully set to Speed test Authentication TR-181 param using WEBPA");
	    } else {
		LOGGER.error("STEP 3: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	    stepNum = "s4";
	    errorMessage = "Unable to retrieve Speed test Authentication using WEBPA";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Verify the Speed test Authentication TR-181 param can be retrieved using WEBPA after setting with 2000 character");
	    LOGGER.info("STEP 4: ACTION : Execute the command : curl -H \"Authorization: Bearer <Sat_Token>\" -k <webpa url>device/mac:<CM_MAC>/config?names=Device.IP.Diagnostics.X_RDKCENTRAL-COM_SpeedTest.Authentication");
	    LOGGER.info("STEP 4: EXPECTED : The returned value should be a 2000 character length");
	    LOGGER.info("**********************************************************************************");
	    response = tapEnv
		    .executeWebPaCommand(device, BroadBandWebPaConstants.WEBPA_PARAM_SPEED_TEST_AUTHENTICATION);
	    errorMessage = "Null/invalid response retrieved from WEBPA for Speed test Authentication";
	    status = CommonMethods.isNotNull(response)
		    && BroadBandTestConstants.SAMPLE_STRING_WITH_2000_CHARACTER.equalsIgnoreCase(response);
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL : The returned value is a 2000 character length string");
	    } else {
		LOGGER.error("STEP 4: ACTUAL : " + errorMessage);
	    }
	    LOGGER.info("**********************************************************************************");
	    tapEnv.updateExecutionStatus(device, testCaseId, stepNum, status, errorMessage, false);

	} catch (Exception e) {
	    errorMessage = errorMessage + e.getMessage();
	    LOGGER.error(errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    false);
	}

	LOGGER.info("ENDING TEST CASE: TC-RDKB-ST-AUT-5001");
    }


}
